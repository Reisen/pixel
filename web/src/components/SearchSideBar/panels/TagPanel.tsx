import React             from 'react';
import { tags }          from '../../../types/image';

import Attribute         from '../../../components/Attribute';
import styles            from './TagPanel.module.css';


interface Props {
    tags?: tags;
}

export default {
    name:    'tags',
    icon:    'tag',
    tooltip: 'Tag List',
    panel:   (props: Props) =>
        <div>
            <h1>Taglist</h1>
            <div className={styles.TagList}>
                {
                    props.tags && Object.keys(props.tags).sort().map(tag => (
                        <Attribute
                            key={tag}
                            icon="tag"
                            name={tag}
                            value={(
                                props.tags &&
                                props.tags[tag] > 1 &&
                                String(props.tags[tag])
                            ) || ''
                            }
                        />
                    ))
                }
            </div>
        </div>
};
