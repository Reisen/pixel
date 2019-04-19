import React, { useState } from 'react';

import Tag                 from '../../components/Tag';
import TextInput           from '../../components/TextInput';
import styles              from './TagPanel.module.css';


interface Props {
    editable?: boolean;
    tags?:     [string, number][];
}

const TagPanel = (props: Props) => {
    const [editMode, setEditMode] = useState<boolean>(false);
    const [deletes, setDeletes]   = useState<string[]>([]);
    const [adds, setAdds]         = useState<string[]>([]);
    const [input, setInput]       = useState<string>('');

    const toggleEditMode = () => {
        setDeletes([]);
        setInput('');
        setEditMode(!editMode)
    }

    const onTagsKeyPress = (e: React.KeyboardEvent<HTMLInputElement>) => {
        if (e.key === 'Enter') {
            setAdds([input, ...adds]);
            setInput('');
        }
    };

    const NewTags = () => {
        return (
            <div className={styles.NewTags}>
                {
                    adds.map(add => (
                        <Tag
                            new
                            key={add}
                            icon="close-circled"
                            name={add}
                            value="NEW"
                            onIcon={() => {}}
                        />
                    ))
                }
            </div>
        );
    }

    return (
        <div>
            <h1>
                Taglist

                {
                    props.editable &&
                        <span onClick={toggleEditMode} className={styles.EditButton}>
                            ({
                                deletes.length > 0 ? 'Save Changes' :
                                adds.length > 0    ? 'Save Changed' :
                                editMode           ? 'Cancel'
                                                : 'Edit'
                            })
                        </span>
                }
            </h1>

            { editMode &&
                <TextInput
                    value={input}
                    onChange={(e) => setInput(e.target.value)}
                    placeholder="Enter Tags Here"
                    onKeyPress={onTagsKeyPress}
                />
            }


            <div className={styles.TagList}>
                { adds.length > 0 && <NewTags /> }

                {
                    props.tags && props.tags.map(tag => (
                        <Tag
                            onIcon={() => {
                                setDeletes([tag[0], ...deletes])
                            }}
                            key={tag[0]}
                            icon={editMode ? "close-circled" : "tag"}
                            name={tag[0]}
                            strikethrough={deletes.includes(tag[0])}
                            value={tag[1].toString()}
                        />
                    ))
                }
            </div>
        </div>
    )
};

export default TagPanel;
