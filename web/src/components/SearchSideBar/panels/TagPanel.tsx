import React     from 'react';
import styles    from './TagPanel.module.css';
import Attribute from '../../../components/Attribute';

interface Props {
    tags?: string[];
}

export default (props: Props) =>
    <div>
        <h1>Taglist</h1>
        <div className={styles.TagList}>
            {
                !props.tags ? null : props.tags.map(tag => (
                    <Attribute key={tag} icon="tag" name={tag} value="1" />
                ))
            }
        </div>
    </div>;
