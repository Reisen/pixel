import React             from 'react';

import Attribute         from '../../../components/Attribute';
import styles            from './TagPanel.module.css';


interface Props {
    tags?: [string, number][];
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
                    props.tags && props.tags.map(tag => (
                        <Attribute
                            key={tag[0]}
                            icon="tag"
                            name={tag[0]}
                            value={tag[1].toString()}
                        />
                    ))
                }
            </div>
        </div>
};
